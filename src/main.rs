use itertools::Itertools;
use rayon::prelude::*;
use std::cmp;
use rand::Rng;
const N: i64 = 100_000_000;

//This function looks for the rank of a number(elemento) inside a slice. it accepts inv:bool  
//to tell if we are looking into the negative array or the possitive one

fn binary_search(slice : &[i64], elemento:i64, inv:bool)->usize
{
    let element = if inv { -elemento } else { elemento };
    let offseta = if inv { -1 as i32} else { 1 };
    let offsetb = if inv { 0 }  else { 1 };

    if slice.len()==0
    {
        return 0;
    }
    else if slice.len()==1
    {
        if element >= slice[0]
        {
            return 1;
        }
        else
        {
            return 0;
        }
    }
    else
    {
        let mut mid = 0;
        let mut start = 0;
        let mut end = slice.len();
        if element>slice[end-1]
        {
            return end;
        }
        else if element < slice[0]
        {
            return 0;
        }
        while (end as i64)-(start as i64) >=0
        {
            mid = (start + end)/2;
            if element == slice[mid]
            {
                if slice[mid] == slice[((mid as i32)+offseta )as usize %slice.len()]{
                    if inv{
                        end = mid-1;
                    }
                    else{
                        start = mid+1;
                    }
                    
                }
                else{
                    return mid+offsetb;
                }
            }
            else if element < slice[mid]
            {
                end = mid-1;
            }
            else
            {
                start = mid + 1
            }
        }
        if inv{
            while slice[mid]>=element{
                if mid ==0 {
                    return 0;
                }
                else{
                    mid-=1;
                }
            }
            return mid+1;
        }
        else{
            while slice[mid]<=element{
                if mid == slice.len()-1 {
                    return slice.len();
                }
                else{
                    mid+=1;
                }
            }
            return mid;
        }
    }
}
//This function builds the indexes list for A,B,C as tuples that represent ranges [low,high)
fn partition(neg : &[i64], pos : &[i64])->(Vec<(usize, usize)>,Vec<(usize, usize)>,Vec<(usize, usize)>)
{
    let mut ar = Vec::new();
    let mut br = Vec::new();
    let mut cr = Vec::new();
    if pos.len()>0
    {
        let lenf= pos.len() as f64;
        let mut loga = 1.0;
        if pos.len() != 1
        {
            loga = lenf.log2(); //so that the division doesnt go to infinity
        }
        let chunks_size = ((lenf/loga).ceil()) as usize;
        let chunks_number = (lenf/(chunks_size as f64)).ceil() as usize;
        let mut a = Vec::new();
        a.push(0);
        let mut b = Vec::new();
        b.push(0);
        for i in 0..chunks_number{
            let index = cmp::min((i+1)*chunks_size,pos.len());
            b.push(index);
            if neg.len()>0{
                let aj = binary_search(neg, pos[index-1], true);
                a.push(neg.len()-aj);
            }
            else{
                a.push(0);
            }
        }
        for i in 0..a.len()-1{
            br.push((b[i],b[i+1]));
            ar.push((a[i],a[i+1]));
            cr.push((a[i]+b[i],a[i+1]+b[i+1]));
        }
        if ar[ar.len()-1].1<neg.len(){
            br.push((b[b.len()-1],b[b.len()-1]));
            ar.push((ar[ar.len()-1].1,neg.len()));
            cr.push((br[br.len()-1].0+ar[ar.len()-1].0,br[br.len()-1].1+ar[ar.len()-1].1));
        }
    }
    else{
        br.push((0,0));
        if neg.len()>0{
            ar.push((0,neg.len()));
            cr.push((0,neg.len()));
        }
        else{
            ar.push((0,0));
            cr.push((0,0));
        }
    }
    return (ar,br,cr);
}
//Function to translate ranges taken in the negative part of the input array
fn translate(range:(usize,usize),leng:usize)->(usize,usize){
    return (leng-range.1,leng-range.0);
}

//sequential merge of two input slices, it squares the number before saving it
fn seq_merge(neg : &[i64], pos : &[i64], res :&mut[i64] )
{
    let mut ok = if neg.len()==0 { false }  else { true };
    let mut i_pos = 0;
    let mut i_neg = 0;
    if ok{
        i_neg = neg.len()-1;
    }
    for k in 0..(neg.len()+pos.len())
    {
        if !ok{
            res[k]=pos[i_pos]*pos[i_pos];
            i_pos+=1;
        }
        else if i_pos >= pos.len(){
            res[k]=neg[i_neg]*neg[i_neg];
            if i_neg == 0{
                ok = false;
            }
            else
            {
                i_neg-=1;
            }
        }
        else if -neg[i_neg]<=pos[i_pos]{
        res[k]=neg[i_neg]*neg[i_neg];
        if i_neg == 0{
            ok = false;
        }
        else
        {
            i_neg-=1;
        }
        }
        else{
        res[k]=pos[i_pos]*pos[i_pos];
        i_pos+=1;
        }
    }
}

fn full_sequential_merge(v : &[i64], buffer: &mut[i64])
{
    let start = std::time::Instant::now();
    let index = binary_search(&v, 0, false);  //we look for the index where we should divide the input array into negatives and positives 
    let (neg, pos) = v.split_at(index); // we divide the input array with the index found in the previous step
    seq_merge(&neg, &pos, buffer); //we perform the sequential merge on the full negative and positive array
    println!("seq: {:?}", start.elapsed());
    assert!(buffer[buffer.len()-1]!=0);
    assert!(buffer.windows(2).all(|w| w[0] <= w[1]));
}

fn full_parallel_merge(v : &[i64],  buffer:&mut Vec<i64>)
{
    fast_tracer::svg("array_sq.svg", || {
    let start = std::time::Instant::now();
    let index = binary_search(&v, 0, false); //find the index of 0
    let (neg, pos) = v.split_at(index); //split into negative and positive
    let(a,b,c) = partition(neg, pos); //build the (Ai,Bi,Ci), array of indexes
    let mut list_slides :Vec<&mut[i64]> = Vec::new(); //because we need to grant concurrent access to C, we create a vector of non overlapping slices in C
    let mut slice = buffer.as_mut_slice();
    for i in  0..c.len() {
        let (now, later) = {slice}.split_at_mut(c[i].1-c[i].0);
        list_slides.push(now);
        slice = later;
    }
    //we perform the parallel merge for all the non overlapping slices of A,B into C
    a.par_iter().zip(b.par_iter()).zip(list_slides.into_par_iter()).for_each(|((aa,bb), cc)| {
        
        let aat = translate(*aa, neg.len());
        seq_merge(&neg[aat.0..aat.1], &pos[bb.0..bb.1], cc)
    });
    println!("par: {:?}", start.elapsed());
    })
    .expect("failed saving svg file");
    assert!(buffer[buffer.len()-1]!=0);
    assert!(buffer.windows(2).all(|w| w[0] <= w[1]));
}

fn main() {
    //rayon::ThreadPoolBuilder::new().num_threads(8).build_global().unwrap();
    let mut rng = rand::thread_rng();
    let v: Vec<i64> = (0..N).map(|_| {
        rng.gen_range(-N/2, N/2)}).sorted().collect();
    let mut buffer: Vec<i64> = std::iter::repeat_with(Default::default)
        .take(v.len()).collect();

    //Sequential version
    full_sequential_merge(&v,&mut buffer);
    println!("output sequential sample: {:?}", &buffer[..10]);
    //parallel version
    buffer.iter_mut().for_each(|x| *x = 0); //we reset the buffer
    full_parallel_merge(&v, &mut buffer);
    println!("output parallel sample: {:?}", &buffer[..10]);
    
}
    




